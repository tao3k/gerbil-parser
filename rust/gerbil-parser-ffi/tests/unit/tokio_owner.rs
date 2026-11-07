use super::{OwnedParsed, Request, TokioParser, TokioParserError, serve};
use crate::NativeError;
use tokio::sync::{mpsc, oneshot};
#[tokio::test]
async fn cancelled_queue_entry_skips_execution_and_owner_drains_on_close() {
    let (requests, incoming) = mpsc::channel(2);
    let (cancelled, receiver) = oneshot::channel();
    drop(receiver);
    requests
        .send(Request::Parse("cancelled".into(), cancelled))
        .await
        .unwrap();
    let (reply, result) = oneshot::channel();
    requests
        .send(Request::Parse("a=1\n".repeat(64), reply))
        .await
        .unwrap();
    assert!(requests.try_reserve().is_err(), "bounded backpressure");
    drop(requests);
    let owner = std::thread::spawn(move || {
        let mut calls = 0;
        assert!(
            serve(incoming, |source| {
                calls += 1;
                assert_eq!(source.lines().count(), 64);
                Err(TokioParserError::Native(NativeError::Transport(17)))
            })
            .is_none()
        );
        calls
    });
    assert!(matches!(
        result.await.unwrap(),
        Err(TokioParserError::Native(NativeError::Transport(17)))
    ));
    assert_eq!(owner.join().unwrap(), 1);
}
#[tokio::test]
async fn shutdown_cancels_later_requests() {
    let (requests, incoming) = mpsc::channel(2);
    let (shutdown, completion) = oneshot::channel();
    requests.send(Request::Shutdown(shutdown)).await.unwrap();
    let (reply, result) = oneshot::channel();
    requests
        .send(Request::Parse(String::new(), reply))
        .await
        .unwrap();
    let owner = std::thread::spawn(move || {
        serve(incoming, |_| panic!("request after shutdown executed"))
            .unwrap()
            .send(Ok(()))
            .unwrap();
    });
    completion.await.unwrap().unwrap();
    assert!(result.await.is_err());
    assert!(
        requests
            .send(Request::Shutdown(oneshot::channel().0))
            .await
            .is_err()
    );
    owner.join().unwrap();
}
#[test]
fn snapshots_and_clients_are_send_and_sync() {
    fn check<T: Send + Sync>() {}
    check::<OwnedParsed>();
    check::<TokioParser>();
}
